// Build don't link: 
// GROUPS passed initialization
class foo {
public:
      operator ++ (); // ERROR - no type or storage class
      operator ++ (int); // ERROR - no type or storage class
      operator ++ (char);		// illegal// ERROR - .*
      operator ++ (short);		// illegal// ERROR - .*
      operator ++ (long);		// illegal// ERROR - .*
};
