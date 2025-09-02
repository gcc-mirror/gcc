/* { dg-do assemble } */
/* { dg-options "-O2" } */

#pragma GCC target "+cmpbr"

long aarch64_fallback_frame_state_tpidr2_0;
unsigned short aarch64_fallback_frame_state_tpidr2_1, aarch64_fallback_frame_state_za_ctx_0;
void aarch64_fallback_frame_state_za_buffer()
{
  long num_slices = aarch64_fallback_frame_state_tpidr2_1;
  if (aarch64_fallback_frame_state_tpidr2_1 > aarch64_fallback_frame_state_za_ctx_0)
    num_slices = aarch64_fallback_frame_state_za_ctx_0;
  __builtin_memcpy((void *)aarch64_fallback_frame_state_tpidr2_0,
         aarch64_fallback_frame_state_za_buffer, num_slices);
}
