// Build don't link:
// Special g++ Options: -w -fpermissive

class foo {};
class bar : foo {
public:
  bar () : () {}
};
