/* { dg-options "-Winline" } */
void quit_mined ();
void bottom_line ();
typedef enum { False, True } FLAG;
inline void
nextfile (FLAG exitiflast)
{
  if (exitiflast)     
    quit_mined ();
  else 
    bottom_line ();
  nextfile (True);
}
