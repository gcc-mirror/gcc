// { dg-do assemble  }
// GROUPS passed error-messages
class foo {
public:
  volatile int () {}// { dg-error "" } 
};
