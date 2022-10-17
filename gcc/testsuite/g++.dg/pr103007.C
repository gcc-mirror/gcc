/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef float MushMeshVector[4];
struct MushMeshQuaternionPair {
  void VectorRotate(MushMeshVector &);
  MushMeshVector m_first;
  MushMeshVector m_second;
};
void 
MushMeshQuaternionPair::
VectorRotate(MushMeshVector &ioVec)  {
  ioVec[2] = (2 - m_first[1] + m_first[3] * 0);
  ioVec[3] = (m_first[3] + m_first[1] - m_first[2] * 0);
  float c = ioVec[2], d = ioVec[3];
  ioVec[2] = (0 - d * m_second[1]);
  ioVec[3] = (2 - c * m_second[1]);
}

