// PR c++/86288
// { dg-do compile { target c++11 } }
// { dg-options "-Wattributes" }

int a [[gnu::aligned(alignof(int))]];
int b [[gnu::__aligned__(alignof(int))]];
int c [[__gnu__::aligned(alignof(int))]];
int d [[__gnu__::__aligned__(alignof(int))]];
int e [[using gnu : aligned(alignof(int))]];		// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
int f [[using gnu : __aligned__(alignof(int))]];	// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
int g [[using __gnu__ : aligned(alignof(int))]];	// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
int h [[using __gnu__ : __aligned__(alignof(int))]];	// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
