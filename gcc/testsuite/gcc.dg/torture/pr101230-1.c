/* { dg-do compile } */
/* { dg-additional-options "-fno-signed-zeros" } */


double distance3d_sqr_pt4d_pt4d(void);

int update_r_k_curr_cluster;
void update_r_k(void) {
  double curr_distance = distance3d_sqr_pt4d_pt4d();
  for (int cluster; cluster; cluster++)
    if (0 < curr_distance) {
      curr_distance = 0;
      update_r_k_curr_cluster = cluster;
    }
}
