// Build don't link: 
// GROUPS passed unions
// anon-union file
// From: "Terry R. Coley" <terry@wag.caltech.edu>
// Date:     Tue, 25 Aug 1992 17:33:29 -0700
// Subject:  possible bug in gcc/g++
// Message-ID: <199208260033.AA19417@brahms.wag.caltech.edu>

typedef enum { BADBINOP = 0, PLUS, MINUS, MULT, DIV, POWR } binoptype;
typedef enum { BADUNOP = 0, NEG = POWR+1, SIN, COS, TAN } unoptype;

typedef struct {
  char *s;
  union {
    binoptype bop;
    unoptype uop;
  };
}
op_to_charp;

op_to_charp BINOPS[] = { {"+", PLUS},
                         {"-", MINUS},
                         {"*", MULT},
                         {"/", DIV},
                         {"^", POWR} };

int main() {
  int dummy;
}
