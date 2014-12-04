int keyring_search(void);
int keydb_search2 (int *hdfound, int *hdcurrent, int *a)
{
    int rc = -1;
    while (rc == -1) {
        if (*a == 1)
            rc = keyring_search ();
        if (rc == -1)
            *hdcurrent++;
	if (!rc)
            *hdfound = *hdcurrent;
    }
    return rc;
}
