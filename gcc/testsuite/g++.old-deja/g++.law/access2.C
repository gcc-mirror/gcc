// { dg-do assemble  }
// GROUPS passed access
// access file
// Message-Id: <9306301534.AA05072@sparc1.cnm.us.es>
// From: juando@cnm.us.es (Juan D. Martin)
// Subject: Compiler lets access to private constructor in template.
// Date: Wed, 30 Jun 93 17:34:10 +0200

template <class T> class Foo
{
private:
    friend class Bar; // To avoid warning.
      Foo(const T &v) {}; // { dg-error "" } private
};


int main()
{
    Foo<int>(1);// { dg-error "" } 
}
