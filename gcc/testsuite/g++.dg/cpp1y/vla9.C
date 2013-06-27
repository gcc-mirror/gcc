// PR c++/57408
// { dg-options "-std=c++1y -pedantic-errors" }

template<typename Callable>
  struct Impl
  {
    Callable func;
    Impl(Callable f) : func(f) { }
    virtual void run() { func(); }
  };

template<typename Callable>
void call(Callable f)
  {
    Impl<Callable>(f).run();
  }

extern "C" int printf(const char*, ...);

int main(){
    int y = 2;
    float fa[2][y];	    // { dg-error "array of array of runtime bound" }
    fa[0][0]=0.8;
    fa[0][1]=1.8;
    auto fx=[&](){
        for(int c=0; c<2; c++){
            printf("use me", fa[0][c]);	// { dg-error "capture of variable-size type" }
        }
    };
    call(fx);
}
