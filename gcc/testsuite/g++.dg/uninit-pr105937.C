// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-options "-O2 -Wall" }
// { dg-skip-if "requires hosted libstdc++ for cwchar" { ! hostedlib } }

#include <stdint.h>
#include <optional>
#include <string_view>

using utf8 = char;

enum
{
    FONT_SIZE_TINY = 2,
    FONT_SIZE_SMALL = 0,
    FONT_SIZE_MEDIUM = 1,
    FONT_SIZE_COUNT = 3
};

constexpr const uint16_t FONT_SPRITE_GLYPH_COUNT = 224;

enum class FontSpriteBase : int16_t
{
    MEDIUM_EXTRA_DARK = -2,
    MEDIUM_DARK = -1,

    TINY = FONT_SIZE_TINY * FONT_SPRITE_GLYPH_COUNT,
    SMALL = FONT_SIZE_SMALL * FONT_SPRITE_GLYPH_COUNT,
    MEDIUM = FONT_SIZE_MEDIUM * FONT_SPRITE_GLYPH_COUNT,
};

struct TTFSurface;

class CodepointView
{
private:
    std::string_view _str;

public:
    class iterator
    {
    private:
        std::string_view _str;
        size_t _index;

    public:
        iterator(std::string_view str, size_t index)
            : _str(str)
            , _index(index)
        {
        }

        bool operator==(const iterator& rhs) const
        {
            return _index == rhs._index;
        }
        bool operator!=(const iterator& rhs) const
        {
            return _index != rhs._index;
        }
        char32_t operator*() const
        {
            return GetNextCodepoint(&_str[_index], nullptr);
        }
        iterator& operator++()
        {
            return *this;
        }
        iterator operator++(int)
        {
            auto result = *this;
            if (_index < _str.size())
            {
                const utf8* nextch;
                GetNextCodepoint(&_str[_index], &nextch);
                _index = nextch - _str.data();
            }
            return result;
        }

        size_t GetIndex() const
        {
            return _index;
        }

        static char32_t GetNextCodepoint(const char* ch, const char** next);
    };

    CodepointView(std::string_view str)
        : _str(str)
    {
    }

    iterator begin() const
    {
        return iterator(_str, 0);
    }

    iterator end() const
    {
        return iterator(_str, _str.size());
    }
};

struct InternalTTFFont;
using TTF_Font = InternalTTFFont;
struct TTFFontDescriptor
{
    const utf8* filename;
    const utf8* font_name;
    int32_t ptSize;
    int32_t offset_x;
    int32_t offset_y;
    int32_t line_height;
    int32_t hinting_threshold;
    TTF_Font* font;
};
using codepoint_t = uint32_t;

#define abstract = 0

struct ITTF
{
    virtual ~ITTF() = default;
    virtual TTFFontDescriptor* ttf_get_font_from_sprite_base(FontSpriteBase spriteBase) abstract;
    virtual TTFSurface* ttf_surface_cache_get_or_add(TTF_Font* font, std::string_view text) abstract;
};

namespace OpenRCT2 {
    struct IContext
    {
        virtual ~IContext() = default;

        virtual ITTF* GetTTF() abstract;
    };
}

static void ttf_draw_string_raw_ttf(OpenRCT2::IContext* context, std::string_view text)
{
    TTFFontDescriptor* fontDesc = context->GetTTF()->ttf_get_font_from_sprite_base(FontSpriteBase::MEDIUM_EXTRA_DARK);
    if (fontDesc->font == nullptr)
    {
        return;
    }

    TTFSurface* surface = context->GetTTF()->ttf_surface_cache_get_or_add(fontDesc->font, text);
    if (surface == nullptr)
        return;
}

namespace UnicodeChar
{
    // Punctuation
    constexpr char32_t leftguillemet = 0xAB;
    constexpr char32_t rightguillemet = 0xBB;
    constexpr char32_t german_quote_open = 0x201E;
    constexpr char32_t quote_open = 0x201C;
    constexpr char32_t quote_close = 0x201D;

    // Dingbats
    constexpr char32_t up = 0x25B2;
    constexpr char32_t small_up = 0x25B4;
    constexpr char32_t right = 0x25B6;
    constexpr char32_t down = 0x25BC;
    constexpr char32_t small_down = 0x25BE;
    constexpr char32_t left = 0x25C0;
    constexpr char32_t tick = 0x2713;
    constexpr char32_t plus = 0x2795;
    constexpr char32_t minus = 0x2796;

    // Emoji
    constexpr char32_t cross = 0x274C;
    constexpr char32_t variation_selector = 0xFE0F;
    constexpr char32_t eye = 0x1F441;
    constexpr char32_t road = 0x1F6E3;
    constexpr char32_t railway = 0x1F6E4;
}; // namespace UnicodeChar


static bool ShouldUseSpriteForCodepoint(char32_t codepoint)
{
    switch (codepoint)
    {
        case UnicodeChar::up:
        case UnicodeChar::down:
        case UnicodeChar::leftguillemet:
        case UnicodeChar::tick:
        case UnicodeChar::cross:
        case UnicodeChar::right:
        case UnicodeChar::rightguillemet:
        case UnicodeChar::small_up:
        case UnicodeChar::small_down:
        case UnicodeChar::left:
        case UnicodeChar::quote_open:
        case UnicodeChar::quote_close:
        case UnicodeChar::german_quote_open:
        case UnicodeChar::plus:
        case UnicodeChar::minus:
        case UnicodeChar::variation_selector:
        case UnicodeChar::eye:
        case UnicodeChar::road:
        case UnicodeChar::railway:
            return true;
        default:
            return false;
    }
}

void ttf_process_string_literal(OpenRCT2::IContext* context, std::string_view text)
{
    CodepointView codepoints(text);
    std::optional<size_t> ttfRunIndex;
    for (auto it = codepoints.begin(); it != codepoints.end(); it++)
    {
        auto codepoint = *it;
        if (ShouldUseSpriteForCodepoint(codepoint))
        {
            if (ttfRunIndex.has_value())
            {
                // Draw the TTF run
                auto len = it.GetIndex() - ttfRunIndex.value();  // { dg-bogus "may be used uninitialized" }
                ttf_draw_string_raw_ttf(context, text.substr(ttfRunIndex.value(), len));
                ttfRunIndex = std::nullopt;
            }

            // Draw the sprite font glyph
        }
        else
        {
            if (!ttfRunIndex.has_value())
            {
                ttfRunIndex = it.GetIndex();
            }
        }
    }
}
