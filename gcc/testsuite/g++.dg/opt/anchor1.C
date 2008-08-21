// { dg-do run }
// { dg-options "-O2" }

// The size of the construction vtable for YFont in YCoreFont was not
// updated to reflect its actual size.  On targets with section anchor
// support, the vtable for YCoreFont was laid out immediately after
// that, but the compiler thought it was about 40 bytes closer to the
// anchor than it actually was.

extern "C" void abort (void);

class refcounted {
public:
    int __refcount;

public:
    refcounted(): __refcount(0) {};
    virtual ~refcounted() {}
};

class YFont : public virtual refcounted {
public:
    virtual ~YFont() {}

    virtual int ascent() const = 0;
};

struct XFontStruct {
};

class YCoreFont : public YFont {
public:
    YCoreFont(char const * name);
    virtual ~YCoreFont();

    virtual int ascent() const { return 2; }

private:
    XFontStruct * fFont;
};

YCoreFont::YCoreFont(char const * name) {
}

YCoreFont::~YCoreFont() {
}

int foo(YCoreFont *ycf)
{
  return ycf->ascent ();
}

int main()
{
  YCoreFont ycf("");
  if (foo(&ycf) != 2)
    abort ();
  return 0;
}
