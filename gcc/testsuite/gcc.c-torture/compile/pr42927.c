typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef enum { READ_SHARED = 0, WRITE_EXCLUSIVE = 1,
    READ_EXCLUSIVE = 2, EXCLUSIVE_ACCESS = 3 } scsires_access_mode;
struct scsires_extent_elem {
    scsires_access_mode mode;
    unsigned relative_address;
    u_int32_t first_block;
    u_int32_t length;
};
typedef struct scsires_extent_elem scsires_extent_elem_t;
struct scsires_extent {
    u_int8_t num_elements;
    scsires_extent_elem_t *elements;
};
typedef struct scsires_extent scsires_extent_t;
unsigned char buf[512];
void scsires_issue_reservation(scsires_extent_t * new_extent)
{
  int i;
  for (i = 0; i < new_extent->num_elements; i++)
    {
      buf[(i * 8)] = new_extent->elements[i].mode;
      buf[(i * 8) + 1] = ((new_extent->elements[i].length >> 16) & 0xff); 
      buf[(i * 8) + 2] = ((new_extent->elements[i].length >> 8) & 0xff);
      buf[(i * 8) + 3] = (new_extent->elements[i].length & 0xff);
      buf[(i * 8) + 4] = ((new_extent->elements[i].first_block >> 24) & 0xff); 
      buf[(i * 8) + 5] = ((new_extent->elements[i].first_block >> 16) & 0xff);
      buf[(i * 8) + 6] = ((new_extent->elements[i].first_block >> 8) & 0xff);
      buf[(i * 8) + 7] = (new_extent->elements[i].first_block & 0xff);
    }
}
