typedef __builtin_va_list va_list;
typedef struct {
    va_list ap;
} ScanfState;
void
GetInt(ScanfState *state, long llval)
{
  *__builtin_va_arg(state->ap,long *) = llval;
  __builtin_va_end(state->ap);
}

