// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

export extern "C++" struct Global {
  Global();
  Global(int);
  void foo();
  static int x;
  struct S;
  template <typename> void a();
  template <typename> static int r;
  template <typename> struct X;

  Global(const Global&);
  Global(double);
  void bar();
  static int y;
  struct T;
  template <typename> void b();
  template <typename> static int s;
  template <typename> struct Y;
};

export struct Module {
  Module();
  Module(int);
  void foo();
  static int x;
  struct S;
  template <typename> void a();
  template <typename> static int r;
  template <typename> struct X;

  Module(const Module&);
  Module(double);
  void bar();
  static int y;
  struct T;
  template <typename> void b();
  template <typename> static int s;
  template <typename> struct Y;
};
