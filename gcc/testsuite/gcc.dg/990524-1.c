/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-O2 -march=pentiumpro" } */

typedef struct t_anim_info {
   char        *new_filename;
   long         first_frame_nr; 
} t_anim_info;
static int
p_frames_to_multilayer(t_anim_info *ainfo_ptr,
                      long range_from, long range_to)
{
  long    l_cur_frame_nr;
  long    l_step, l_begin, l_end;
  int  l_tmp_image_id;
  int  l_new_image_id;
  if(range_from > range_to)
  {
    l_step  = -1;      
    if(range_to < ainfo_ptr->first_frame_nr)
    { l_begin = ainfo_ptr->first_frame_nr; 
     }
  }
  else
  {
    l_step  = 1;       
  }
  l_cur_frame_nr = l_begin;
  while(1)
  {
    if(ainfo_ptr->new_filename == ((void *)0) )
    if(l_tmp_image_id < 0)
      gimp_image_delete(l_tmp_image_id);
    if(l_cur_frame_nr == l_end)
       break;
    l_cur_frame_nr += l_step;
  }
  return 0;
}
