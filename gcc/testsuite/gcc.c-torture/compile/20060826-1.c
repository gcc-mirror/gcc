typedef _Complex double S;
S bar (void);
void quux (S, S);
void foo (void)
{
 quux (bar(), bar());
}
