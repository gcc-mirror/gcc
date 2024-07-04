#include <stdlib.h>

struct {
    long sqlcode;
} sqlca;


struct data_record {
    int dummy;
    int a[100];
} *data_ptr, data_tmp;


int
num_records()
{
    return 1;
}


void
fetch()
{
    static int fetch_count;

    __builtin_memset(&data_tmp, 0x55, sizeof(data_tmp));
    sqlca.sqlcode = (++fetch_count > 1 ? 100 : 0);
}


void
load_data() {
    struct data_record *p;
    int num = num_records();

    data_ptr = malloc(num * sizeof(struct data_record));
    __builtin_memset(data_ptr, 0xaa, num * sizeof(struct data_record));

    fetch();
    p = data_ptr;
    while (sqlca.sqlcode == 0) {
        *p++ = data_tmp;
        fetch();
    }
}

int
main(void)
{
    load_data();
    if (sizeof (int) == 2 && data_ptr[0].dummy != 0x5555)
      abort ();
    else if (sizeof (int) > 2 && data_ptr[0].dummy != 0x55555555)
      abort ();
    exit (0);
}
