// { dg-do assemble  }
// { dg-options "-w -fpermissive" }

class foo {};
class bar : foo {
public:
  bar () : () {}
};
