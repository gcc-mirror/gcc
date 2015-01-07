// PR c++/63657
// { dg-options "-Wunused-variable" }

class Bar
{
  virtual ~Bar() {}
};
Bar& getbar();
void bar()
{
  Bar& b = getbar();		// { dg-warning "unused" }
}
