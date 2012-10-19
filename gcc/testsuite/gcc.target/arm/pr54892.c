/* { dg-do compile } */

int set_role(unsigned char role_id, short m_role)
{
  return __sync_bool_compare_and_swap(&m_role, -1, role_id);
}

