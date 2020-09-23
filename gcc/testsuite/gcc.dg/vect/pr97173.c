/* { dg-do compile } */

typedef struct {
  char *track;
  char *clocks;
  char *fm;
  char *weak;  
} disk_t;

disk_t disk_update_tlens_d;
int disk_update_tlens_d_0;

void disk_update_tlens() {
  disk_update_tlens_d.track = disk_update_tlens_d.clocks =
      disk_update_tlens_d.track + disk_update_tlens_d_0;
  disk_update_tlens_d.fm = disk_update_tlens_d.clocks + disk_update_tlens_d_0;
  disk_update_tlens_d.weak = disk_update_tlens_d.fm;
  disk_update_tlens_d.track[2] = 5;
}
