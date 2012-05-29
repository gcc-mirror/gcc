// { dg-options "-O" }

template<typename T> class OwnPtr {
public:
    ~OwnPtr();
};
template<class T> class GlyphMetricsMap {
public:
    GlyphMetricsMap() { }
    OwnPtr<int> m_pages;
};
class SimpleFontData {
public:
    void boundsForGlyph() const;
};
inline __attribute__((__always_inline__))
void SimpleFontData::boundsForGlyph() const
{
  new GlyphMetricsMap<int>;
}
void offsetToMiddleOfGlyph(const SimpleFontData* fontData)
{
  fontData->boundsForGlyph();
}
int main() {}
