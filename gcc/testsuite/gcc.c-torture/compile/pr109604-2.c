/* This used to ICE after PHI-OPT because of the "empty block" and diamond form bbs
   was not checking to make sure each bbs were only coming from the one bb. */

struct {
  int second;
} selectPlayer_playerRes;
int selectPlayer_playerRes_0;
int selectPlayer() {
  if (selectPlayer_playerRes_0 && selectPlayer_playerRes.second >= 0)
    return selectPlayer_playerRes.second;
  else
    return -1;
}
