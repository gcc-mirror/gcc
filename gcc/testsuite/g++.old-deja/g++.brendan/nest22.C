// Build don't link: 
// GROUPS passed nested-classes
class foo {
public:
   typedef int bar;
   foo::bar    fb; // this line causes a syntax error!
};
