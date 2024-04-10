/* { dg-do compile } */
/* { dg-options "-g -funroll-loops" } */

typedef unsigned short uint16;

void intrapred_chroma_plane(uint16 ***mb_preds, int* max_imgpel_values, int crx, int cry, int px) {
  for (int uv = 0; uv < 2; uv++) {
    uint16 **mb_pred = mb_preds[uv + 1];
    uint16 **predU2 = &mb_pred[px - 2];
    uint16 *upPred = &mb_pred[px][px];
    int max_imgpel_value = max_imgpel_values[uv];

    int ih = upPred[crx - 1];
    for (int i = 0; i < crx*3; ++i)
      ih += upPred[crx*3];

    int iv = (mb_pred[cry - 1][px+1]);
    for (int i = 0; i < cry - 1; ++i) {
      iv += (i + 1) * (*(mb_preds[uv][0]) - *(*predU2--));
    }

    for (int j = 0; j < cry; ++j)
      for (int i = 0; i < crx; ++i)
        mb_pred[j][i] = (uint16) (max_imgpel_value * ((i * ih + iv)));
  }
}
