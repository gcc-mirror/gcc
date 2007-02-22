// { dg-do compile }
// { dg-options "-fshort-enums -w" }

// PR c++/20008

// We failed to compile this because CFG cleanup left the switch
// statement intact, whereas expand_case expected at least one
// in-range case to remain.

typedef enum _SECStatus {
  SECWouldBlock = -2,
  SECFailure = -1,
  SECSuccess = 0
} SECStatus;

typedef enum {
  SEC_ERROR_BAD_SIGNATURE = (-0x2000) + 10
} SECErrorCodes;

void g(void);
void f(SECStatus status)
{
  switch( status )
    {
    case SEC_ERROR_BAD_SIGNATURE :
      // This case can be optimized away in C++ (but apparently not in
      // C), because the enum type is defined with a narrow range.
      g();
      break ;
    }
}
