typedef struct
{
  enum { NotConnected = 0 } conn_state;
  unsigned int conn_hndl;
} AEP_CONNECTION_ENTRY;

static AEP_CONNECTION_ENTRY aep_app_conn_table[256];

void aep_mod_exp (void)
{
  int count;

  for (count = 0; count < 256; count++)
    {
      aep_app_conn_table[count].conn_state = NotConnected;
      aep_app_conn_table[count].conn_hndl = 0;
    }
}

