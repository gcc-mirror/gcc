//Build don't link:
#include <string>

class t {
public:
       t(const string& s) : s_(s) {}
       string s_;
       static t* t_;
};

t* t::t_;

t* makeT()
{
       return new t("test");
       return t::t_ ? t::t_ :
        t::t_ = new t("test");
}
