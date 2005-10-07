int Fdisplay_buffer   (int buffer)
{
  if (((struct buffer *) ((unsigned int) buffer)) ==
      (0,(struct buffer *) ((unsigned int) ((buffer) & 1))))
    return 1;
}
