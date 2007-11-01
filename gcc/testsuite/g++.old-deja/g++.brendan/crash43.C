// { dg-do assemble }
// { dg-xfail-if "" { sparc64-*-elf } { "*" } { "" } }
// { dg-options "-g" }
// GROUPS passed old-abort
extern "C" { typedef int jmp_buf[12]; }

enum Error { NO_ERROR };
class ErrorHandler
{
    ErrorHandler *previous;
    static ErrorHandler *error_stack;
    jmp_buf error_buffer;
protected:
    static void pop()
    {
	error_stack = error_stack->previous;
    }
public:
    jmp_buf *push()
    {
	previous = error_stack;
	error_stack = this;
	return &error_buffer;
    }
};
