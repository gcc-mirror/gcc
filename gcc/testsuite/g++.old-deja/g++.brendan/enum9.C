// { dg-do assemble  }
// GROUPS passed enums
enum fig {
    figgy,
    pudding,
}; // { dg-error "" } comma

class X {
public:
    static fig (*open)(void *thing, const char *filename);
    static fig (*parse)(void *thing);
};

enum fig (*X::open)(void *thing, const char *filename) = 0;
fig (*X::parse)(void *thing) = 0;
