/* We used to ICE as we did not mark a Vop for rename as
   we changed a function call to a normal modify statement
   while folding exp(0.0); */

double exp(double);
void f0(void);
void f(double);
typedef struct Parser {
    int x;
    char *s;
} Parser;
static double pop(Parser *p) {
    if (p->s[0] <= 0) {
	f0();
	return 0;
    }
    --p->x;
    return 0;
}
static void evalFactor(Parser *p) {
    while (p->x)
	f(exp(pop(p)));
}
static void evalTerm(Parser *p) {
    while (p->s[0])
	evalFactor(p);
}
static void evalExpression(Parser *p) {
    evalTerm(p);
    while (p->s[0])
	evalTerm(p);
}
void evalPrimary(Parser *p) {
    if (p->s)
	return;
    evalExpression(p);
}

