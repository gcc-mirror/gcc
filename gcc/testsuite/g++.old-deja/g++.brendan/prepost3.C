// Build don't link: 
// GROUPS passed prefix-postfix
class Y {
public:
   friend Y operator++ (Y&);
   friend Y operator++ (Y&, char);	// illegal// ERROR - .*
};
