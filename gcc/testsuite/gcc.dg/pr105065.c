/* PR middle-end/105065 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef struct
{
	char filler[17];
} big_struct;

big_struct dummy(int size, char array[size]);

int main()
{
	dummy(0, 0);
}

