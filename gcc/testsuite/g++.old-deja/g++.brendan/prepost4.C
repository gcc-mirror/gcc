// { dg-do assemble  }
// GROUPS passed prefix-postfix
class Y {
public:
   friend Y operator++ (Y&);

   // This is legal---it's a good test to make sure that grokfndecl's
   // checking of the arguments is sane.
   friend Y operator++ (Y&, int);
};
