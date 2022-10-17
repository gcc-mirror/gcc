/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int globus_i_GLOBUS_GRIDFTP_SERVER_debug_handle_1;
int globus_l_gfs_ipc_unpack_data__sz;
void globus_i_GLOBUS_GRIDFTP_SERVER_debug_printf(const char *, ...);
static void globus_l_gfs_ipc_unpack_cred(int len) {
  if (globus_i_GLOBUS_GRIDFTP_SERVER_debug_handle_1)
    globus_i_GLOBUS_GRIDFTP_SERVER_debug_printf("", __func__);
}
static void globus_l_gfs_ipc_unpack_data(int len) {
  for (; globus_l_gfs_ipc_unpack_data__sz;)
    len--;
  len -= 4;
  len -= 4;
  globus_l_gfs_ipc_unpack_cred(len);
}
void globus_l_gfs_ipc_reply_read_body_cb(int len)
{ globus_l_gfs_ipc_unpack_data(len); }
