// { dg-do assemble  }
// GROUPS passed prefix-postfix
class Y {
public:
   friend Y operator++ (Y&);
   friend Y operator++ (Y&, char);  // { dg-error "13:postfix .Y operator\\+\\+\\(Y&, char\\). must have .int. as its second argument" }
};
