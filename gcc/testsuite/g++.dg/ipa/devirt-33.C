/* Verify we do not devirtualize wrongly to __cxa_pure_virtual */

/* { dg-do run } */
/* { dg-options "-O2 -std=c++11"  } */


inline void* operator new(__SIZE_TYPE__ s, void* buf) throw() {
   return buf;
} 

class A {

private:
  struct Base {
      virtual ~Base() {}
      virtual Base *Clone(void *buf) const = 0;
      virtual float *Allocate(__SIZE_TYPE__ count) = 0;
   };

 struct Value : Base {
    virtual ~Value (){}
    Base *Clone(void* buf)  const override {
      return new (buf) Value(); 
    } 

    float *Allocate(__SIZE_TYPE__ count) override {
      return new float[count];
    }
  };

public:
  A() {
    new (buffer_) Value();
  }
  A(const A& other) {
    other.ptr()->Clone(buffer_);
  }

  float *Allocate() {
     return ptr()->Allocate(100);
  }
  const Base *ptr() const { return reinterpret_cast<const Base*>(buffer_);}
  Base *ptr()  { return reinterpret_cast< Base*>(buffer_);}

private:
  alignas(16) char buffer_[1024];
};


struct B {
 B (const A& a) : a_(a) {
    buff_ = a_.Allocate();
 }

 float *buff_;
 A a_;
};

struct Dummy {
 int i;
};

struct D : public Dummy {
   __attribute__((noinline)) D( const A&a);

  B b_;
};

D::D(const A&a) : b_(a) {}
 
int main()
{
   A a;
   D d(a); 

   return 0;
}

