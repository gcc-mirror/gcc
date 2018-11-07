import imports.link14074z;

void replaceAllWith(RE)(RE re)
if (is(RE == S!()) || true)
{
    auto m = RegexMatch!()(re);
}

struct RegexMatch()
{
    this(RE)(RE prog)
    {
        enum x = is(RE == S!());
    }
}

struct S()  // StaticRegex
{
    alias Matcher = BTM!();
    alias M = Matcher!();
}

struct Input()
{
    struct L
    {
        auto loopBack()
        {
            return Input();
        }
    }
    auto loopBack()
    {
        return L();
    }
}

template BTM()  // BacktrackingMatcher
{
    struct BTM(Stream = Input!())
    {
        Stream s;
        dchar front;

        this(Stream stream)
        {
        }

        auto bwdMatcher(ref BTM matcher)
        {
            alias BMTempl = .BTM!();
            alias BM = BMTempl!(typeof(s.loopBack()));
            auto m = BM(s.loopBack());
        }
    }
}
