// Build don't link: 
// GROUPS passed error-messages
class foo {
public:
  ~bar () {}// ERROR -  destructor `bar' must match class name `foo'.*
};

