// { dg-do run  }
extern "C" int printf(const char *, ...);
void *vp;
int fail = 0;

class Foo {
public:
   virtual void setName() {
     printf("Foo at %x\n", this);
     if (vp != (void*)this)
       fail = 1;
   }
};

class Bar : public Foo {
public:
  virtual void init(int argc, char **argv) {
    printf("Bar's Foo at %x\n", (Foo*)this);
    vp = (void*)(Foo*)this;
    setName();
  }
};

class Barf : virtual public Bar {
public:
  virtual void init(int argc, char **argv) { Bar::init(argc, argv); }
};

class Baz : virtual public Bar, virtual public Barf {
public:
  virtual void init(int argc, char **argv) { Barf::init(argc, argv); }
};

Bar *theBar = new Baz();

int main(int argc, char **argv) {
   theBar->init(argc, argv);
   return fail;
}
