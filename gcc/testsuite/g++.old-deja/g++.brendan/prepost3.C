// { dg-do assemble  }
// GROUPS passed prefix-postfix
class Y {
public:
   friend Y operator++ (Y&);
   friend Y operator++ (Y&, char);	// illegal// { dg-error "" } .*
};
