/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */
/* This was extracted from coremark.  */


typedef signed short ee_s16;
typedef struct list_data_s
{
    ee_s16 data16;
    ee_s16 idx;
} list_data;

typedef struct list_head_s
{
    struct list_head_s *next;
    struct list_data_s *info;
} list_head;


list_head *
core_list_find(list_head *list, list_data *info)
{
    if (info->idx >= 0)
    {
        while (list && (list->info->idx != info->idx))
            list = list->next;
        return list;
    }
    else
    {
        while (list && ((list->info->data16 & 0xff) != info->data16))
            list = list->next;
        return list;
    }
}

/* There was an unnecessary assignment to the return value until
   recently.  Scan for that in the resulting output.  */
/* { dg-final { scan-assembler-not "li\\ta0,0" } } */

