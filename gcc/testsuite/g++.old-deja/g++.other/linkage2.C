// Build don't link:
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de>    
extern "C" 
{
typedef struct {int dummy[10];} *GDBM_FILE;
extern GDBM_FILE gdbm_open();
}

typedef struct { int dummy[10]; } *FAIL_FILE;
extern FAIL_FILE fail_open(); // ERROR - non-local function

typedef struct { int dummy[10]; } *SUCCESS_FILE, S;
extern SUCCESS_FILE success_open();

