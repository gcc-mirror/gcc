/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mpowerpc64 -mdynamic-no-pic" } */

long long knight_attacks[64];
long long InitializeAttackBoards(void);

int main()
{
	return InitializeAttackBoards();
}

long long InitializeAttackBoards(void)
{

  int i,j;

  for(i=0;i<64;i++) { }

  for(i=0;i<64;i++) {
    knight_attacks[i]=0;
    for(j=0;j<8;j++) {
      knight_attacks[i]= 0;
    }
  }

  return knight_attacks[0];

}

