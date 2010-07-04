// { dg-do compile }

extern void xread(void *);
class test
{
public:
    test(void);
};
test::test(void)
{
  union {
      char pngpal[1];
  };
  xread(pngpal);
}

