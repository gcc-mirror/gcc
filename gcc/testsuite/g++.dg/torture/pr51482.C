// { dg-do compile }

typedef enum { CLASS_IN = 1, CLASS_OUT = -1 } FERGUSON_KEY_CLASS, BEZIER_KEY_CLASS;
typedef class flag_interface { } VECT3DF_SIMPLE;
typedef struct vect3df {
    float x,y,z;
} VECT3DF, VECT;
typedef struct vect4df : public vect3df {
    float w;
} VECT4DF, WVECT;
typedef class llist_item { } ANIM_KEY;
typedef class anim_track : public flag_interface, public llist_item { } ANIM_KEY_BEZ;
typedef class anim_track_bezier : public anim_track { } ANIM_KEY_BEZ_WVECT;
typedef class anim_track_bez_wvect : public anim_track_bezier {
    WVECT * tangent(int kn, BEZIER_KEY_CLASS key_class, WVECT *p_tn);
} ANIM_TRACK_BEZ_WVECT;
WVECT * anim_track_bez_wvect::tangent(int kn, BEZIER_KEY_CLASS key_class, WVECT *p_tn)
{
  float bias,continuity,tension,tn1,bp1;
  WVECT *p_p0,*p_p1,*p_p2,         t1,         g1,g2,g3;
  g1.x = (p_p1->x - p_p0->x)*bp1;
  g1.y = (p_p1->y - p_p0->y)*bp1;
  g1.z = (p_p1->z - p_p0->z)*bp1;
  g1.w = (p_p1->w - p_p0->w)*bp1;
  bp1 = (0.5f + key_class*0.5f*continuity);
  p_tn->x = (g1.x + g3.x*bp1)*tn1;
  p_tn->y = (g1.y + g3.y*bp1)*tn1;
  p_tn->z = (g1.z + g3.z*bp1)*tn1;
  p_tn->w = (g1.w + g3.w*bp1)*tn1;
  return 0;
}
