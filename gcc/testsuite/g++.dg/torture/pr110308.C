/* { dg-do compile } */

int channelCount, decodeBlock_outputLength;
struct BlockCodec {
  virtual int decodeBlock(const unsigned char *, short *);
};
struct ms_adpcm_state {
  char predictorIndex;
  int sample1;
  ms_adpcm_state();
};
bool decodeBlock_ok;
void encodeBlock() { ms_adpcm_state(); }
struct MSADPCM : BlockCodec {
  int decodeBlock(const unsigned char *, short *);
};
void decodeSample(ms_adpcm_state, bool *);
int MSADPCM::decodeBlock(const unsigned char *, short *) {
  ms_adpcm_state decoderState[2];
  ms_adpcm_state *state[2];
  state[0] = &decoderState[0];
  if (channelCount == 2)
    state[1] = &decoderState[0];
  short m_coefficients[state[1]->predictorIndex];
  for (int i = 0; i < channelCount; i++)
    ++state[i]->sample1;
  decodeSample(*state[1], &decodeBlock_ok);
  return decodeBlock_outputLength;
}