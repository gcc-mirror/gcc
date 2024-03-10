/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

#include <stdint.h>
#include <stddef.h>

typedef struct
{
    uint16_t pgv1_size;
    uint16_t pgv1_flags1;
    uint16_t pgv1_flags2;
    uint16_t pgv1_flags3;
    uint16_t pgv1_nslots;
    uint32_t pgv1_generic;
} page_v1_t;

typedef struct
{
    page_v1_t pgv2_hdr;
    int64_t pgv2_next;
    int64_t pgv2_prev;
} page_v2_t;

typedef struct
{
    union
    {
        struct
        {
            uint16_t pgv0_size;
            uint16_t pgv0_flags;
        };
        uint32_t pgv0_nslots;
    };
    uint32_t pgv0_mode;
    uint32_t pgv0_next;
} page_v0_t;

typedef struct
{
    uint16_t sl4_off;
    uint16_t sl4_len;
} slot4_t;

typedef struct
{
    uint32_t sl8_off;
    uint32_t sl8_len;
} slot8_t;
extern int64_t cur_rowid;

extern uint8_t *pg_alloc(size_t pg_size);
extern void mem_move(void *dst, const void *src, size_t len);
extern void pg_expand(uint8_t *row_in, uint8_t *row_out, uint16_t pg_version);
extern void pg_reorg(uint32_t flags, uint8_t *pg_old, uint16_t slotnum, uint8_t *row_data, uint64_t cur_partp);

void pg_reorg(uint32_t flags, uint8_t *pg_old, uint16_t slotnum, uint8_t *row_data, uint64_t cur_partp)
{
    uint16_t pg_version;
    size_t pg_size = ((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800)));
    uint8_t *pg_new = pg_alloc(pg_size);
    uint8_t *old_slot = ((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (&(pg_old)[((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) - sizeof(int64_t) - (1*sizeof(slot8_t))]) : ((((page_v0_t *)(pg_old))->pgv0_mode & 1) ? (&(pg_old)[((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) - sizeof(int64_t) - (1*sizeof(slot4_t))]) : (&(pg_old)[((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) - sizeof(int32_t) - (1*sizeof(slot4_t))])));
    uint8_t *new_slot = ((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (&(pg_new)[((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) - sizeof(int64_t) - (1*sizeof(slot8_t))]) : ((((page_v0_t *)(pg_new))->pgv0_mode & 1) ? (&(pg_new)[((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) - sizeof(int64_t) - (1*sizeof(slot4_t))]) : (&(pg_new)[((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) - sizeof(int32_t) - (1*sizeof(slot4_t))])));

    if (flags == 2)
    {

        pg_version = ((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_generic >> 24) : ((uint32_t)((page_v0_t *)(pg_old))->pgv0_next >> 24));
    }

    int64_t sav_rowid = cur_rowid;


    for (uint16_t i = 1; i <= ((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_nslots) : (((page_v0_t *)(pg_old))->pgv0_nslots)); i++,
         (old_slot -= ((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? sizeof(slot8_t) : sizeof(slot4_t))), (new_slot -= ((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? sizeof(slot8_t) : sizeof(slot4_t))))
    {
        size_t length;


        if (flags == 1 && (slotnum == 0 || i == slotnum))
        {
            uint16_t slotlen = ((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(old_slot))->sl8_len & 0x000FFFFF) : (((slot4_t *)(old_slot))->sl4_len & 0x3FFF));
            ((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(new_slot))->sl8_len = (((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(new_slot))->sl8_len & 0xFFF00000) : (((slot4_t *)(new_slot))->sl4_len & 0xC000)) | ((slotlen) & 0x000FFFFF))) : (((slot4_t *)(new_slot))->sl4_len = (((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(new_slot))->sl8_len & 0xFFF00000) : (((slot4_t *)(new_slot))->sl4_len & 0xC000)) | ((slotlen) & 0x3FFF))));
            length = ((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(old_slot))->sl8_len & 0x000FFFFF) : (((slot4_t *)(old_slot))->sl4_len & 0x3FFF));
        }
        else
        {
            length = ((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(old_slot))->sl8_len & 0x000FFFFF) : (((slot4_t *)(old_slot))->sl4_len & 0x3FFF));
            ((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(new_slot))->sl8_len = (((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(new_slot))->sl8_len & 0xFFF00000) : (((slot4_t *)(new_slot))->sl4_len & 0xC000)) | ((length) & 0x000FFFFF))) : (((slot4_t *)(new_slot))->sl4_len = (((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)(new_slot))->sl8_len & 0xFFF00000) : (((slot4_t *)(new_slot))->sl4_len & 0xC000)) | ((length) & 0x3FFF))));
        }


        if (((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)old_slot)->sl8_off) : (((slot4_t *)old_slot)->sl4_off)) == 0)
        {
            ((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)new_slot)->sl8_off = (0)) : (((slot4_t *)new_slot)->sl4_off = (0)));
            continue;
        }

        ((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((slot8_t *)new_slot)->sl8_off = (0)) : (((slot4_t *)new_slot)->sl4_off = (0)));

        if (flags == 2)
        {
            cur_rowid = (((((cur_partp) & 0x400000000) ? 1 : 0)) ? (uint64_t)(((((int64_t)((((uint64_t)(sav_rowid) & ((uint64_t)1 << 63)) ? (int64_t)(((sav_rowid) & 0x7FFFFFFFFFFF0000) >> 16) : (int64_t)(((sav_rowid) & 0xFFFFFF00) >> 8)))) << 16) + (i)) | ((uint64_t)1 << 63)) : (uint64_t)((((int32_t)((((uint64_t)(sav_rowid) & ((uint64_t)1 << 63)) ? (int64_t)(((sav_rowid) & 0x7FFFFFFFFFFF0000) >> 16) : (int64_t)(((sav_rowid) & 0xFFFFFF00) >> 8)))) << 8) + (i)));
            pg_expand(((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (((uint8_t*)(pg_old)) + ((slot8_t *)old_slot)->sl8_off) : (((uint8_t*)(pg_old)) + ((slot4_t *)old_slot)->sl4_off)), row_data, pg_version); /* { dg-bogus "uninitialized" } */
        }
        else
        {

            mem_move(((((((page_v1_t *)(pg_new))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_new))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_new))->pgv0_size & 0xF800))) > 0x4000) ? (((uint8_t*)(pg_new)) + ((slot8_t *)new_slot)->sl8_off) : (((uint8_t*)(pg_new)) + ((slot4_t *)new_slot)->sl4_off)), ((((((page_v1_t *)(pg_old))->pgv1_flags3 == 3) ? (((page_v1_t *)(pg_old))->pgv1_size*1024) : (uint16_t)(0x0800 + (((page_v0_t *)(pg_old))->pgv0_size & 0xF800))) > 0x4000) ? (((uint8_t*)(pg_old)) + ((slot8_t *)old_slot)->sl8_off) : (((uint8_t*)(pg_old)) + ((slot4_t *)old_slot)->sl4_off)), length);
        }
    }
}
