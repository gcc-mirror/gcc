/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2" } */

unsigned long long GetTimeFromFrames(int);
unsigned long long GetMicroSeconds();

void DequeueEvent(unsigned frame) {
  long long frame_time = GetTimeFromFrames(frame);
  unsigned long long current_time = GetMicroSeconds();

  DequeueEvent(frame_time < current_time ? 0 : frame_time - current_time);
}
