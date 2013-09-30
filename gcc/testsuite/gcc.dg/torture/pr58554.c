/* { dg-do run } */

extern void abort (void);
void __attribute__((noinline,noclone))
clear_board(unsigned char *board, int board_size)
{
  int k;
  for (k = 0; k < 421; k++)
    if (k < board_size )
      board[k] = 3;
}
int main()
{
  unsigned char board[421];
  board[420] = 1;
  clear_board (board, 420);
  if (board[420] != 1)
    abort ();
  return 0;
}
