// Build don't link:
// Origin: Benjamin Pflugmann <philemon@spin.de>
// Special g++ Options: -O

typedef const char *(func_type)();

class
{
public:
  func_type *Function;
  const func_type* function(void) { return Function; } // ERROR - qualifiers
} action;

void work(const char *source)
{
  work( action.function()() );
}
