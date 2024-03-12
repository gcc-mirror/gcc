// P2621R2 - UB? In My Lexer?
// { dg-do compile }

// Line splicing can form a universal-character-name [lex.charset].
int \\
u\
0\
3\
9\
1 = 0;

// [cpp.concat] Concatenation can form a universal-character-name.
#define CONCAT(x,y) x##y

int CONCAT(\,u0393)=0;
