// Build don't link:
// Origin: Benjamin Pflugmann <philemon@spin.de>
// Special g++ Options: -O

// DR 295 allows qualification via typedef

typedef const char *(func_type)();

class
{
public:
  func_type *Function;
  // The following is DR 295 dependent
  const func_type* function(void) { return Function; } // ERROR - constifying
  volatile func_type* functionv(void); // ERROR - qualifier
} action;

void work(const char *source)
{
  work( action.function()() );
}
