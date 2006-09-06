/* { dg-do compile } */

/* We were  ICE because we wanted to check the type of the
   elements of a conditional before we knew it was a conditional.  */

struct player_spaceship
{
  _Bool structure[32];
};
struct player
{
  struct player_spaceship spaceship;
};
struct packet_spaceship_info
{
  char structure[32 + 1];
};
send_spaceship_info (void)
{
  int j;
  struct player *pplayer;
  struct packet_spaceship_info info;
  struct player_spaceship *ship = &pplayer->spaceship;
  for (j = 0; j < 32; j++)
  {
    info.structure[j] = ship->structure[j] ? '1' : '0';
  }
  lsend_packet_spaceship_info (&info);
}


