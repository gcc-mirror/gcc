// PR 10202
// { dg-do compile }
// { dg-options -O0 }

extern struct _smtp_account smtp_accounts[];
typedef struct _smtp_account {
        int flags;
} Smtp_Account;

void get_smtp_host_info ()
{
	if (smtp_accounts[0].flags & 0x01)
		get_smtp_host_info();
}
