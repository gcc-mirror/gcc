// Build don't link:
// Origin: Benjamin Pflugmann <philemon@spin.de>
// Special g++ Options: -O

// DR 295 allows qualification via typedef

typedef const char *(func_type)();

class
{
public:
  func_type *Function;
  const func_type* function(void) { return Function; } // ok constifying
  volatile func_type* functionv(void); // WARNING - qualifier
} action;

void work(const char *source)
{
  work( action.function()() );
}
