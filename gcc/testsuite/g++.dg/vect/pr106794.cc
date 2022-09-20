/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-additional-options "-march=bdver2" { target x86_64-*-* i?86-*-* } } */

template <class T> struct Vector3 {
  Vector3();
  Vector3(T, T, T);
  T length() const;
  T x, y, z;
};
template <class T>
Vector3<T>::Vector3(T _x, T _y, T _z) : x(_x), y(_y), z(_z) {}
Vector3<float> cross(Vector3<float> a, Vector3<float> b) {
  return Vector3<float>(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z,
                        a.x * b.y - a.y * b.x);
}
template <class T> T Vector3<T>::length() const { return z; }
int generateNormals_i;
float generateNormals_p2_0, generateNormals_p0_0;
struct SphereMesh {
  void generateNormals();
  float vertices;
};
void SphereMesh::generateNormals() {
  Vector3<float> *faceNormals = new Vector3<float>;
  for (int j; j; j++) {
    float *p0 = &vertices + 3, *p1 = &vertices + j * 3, *p2 = &vertices + 3,
          *p3 = &vertices + generateNormals_i + j * 3;
    Vector3<float> v0(p1[0] - generateNormals_p0_0, p1[1] - 1, p1[2] - 2),
        v1(0, 1, 2);
    if (v0.length())
      v1 = Vector3<float>(p3[0] - generateNormals_p2_0, p3[1] - p2[1],
                          p3[2] - p2[2]);
    else
      v1 = Vector3<float>(generateNormals_p0_0 - p3[0], p0[1] - p3[1],
                          p0[2] - p3[2]);
    Vector3<float> faceNormal = cross(v0, v1);
    faceNormals[j] = faceNormal;
  }
}
