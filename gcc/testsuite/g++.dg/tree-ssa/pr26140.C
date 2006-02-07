/* { dg-do compile } */

struct Pitch
{
  int notename_;
};
struct Audio_note
{
  Audio_note (Pitch p);
};
void create_audio_elements ()
{
  Pitch *pit;
  new Audio_note (*pit);
}
