// { dg-do assemble  }
class foo {
protected:
  void __duplicate ();

};

class bar : public virtual foo  {
protected:
    void __duplicate() {
       foo::__duplicate ();
    }
};

class oops : public virtual bar {
protected:
    void __duplicate() {
       foo::__duplicate ();
    }
};
