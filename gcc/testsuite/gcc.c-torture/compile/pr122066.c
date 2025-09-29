/* PR target/122066 -- adddi3/subdi3 mishandle POST_INC/PRE_DEC dest on m68k */

struct {
  long long wp_ssd[3];
  long long wp_sum[3];
} m_lowres;
void calcAdaptiveQuantFrame() {
  for (int i = 0; i < 3; i++) {
    long sum = m_lowres.wp_sum[i];
    long long ssd = m_lowres.wp_ssd[i];
    m_lowres.wp_ssd[i] = ssd - sum;
  }
  for (int i = 0; i < 3; i++) {
    long sum = m_lowres.wp_sum[i];
    long long ssd = m_lowres.wp_ssd[i];
    m_lowres.wp_ssd[i] = ssd + sum;
  }
}
