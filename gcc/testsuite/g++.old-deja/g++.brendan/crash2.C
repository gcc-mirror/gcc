// Build don't link: 
// GROUPS passed old-abort
// The compiler used to crash on this example.

class x {
public:
  x();
  static const x y[23];
};
const x x::y[23];
