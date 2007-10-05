/* { dg-do compile } */

/* This used to fail with type-checking enabled because we didn't
   expect OBJ_TYPE_REF expressions.  */

class QObject {};
class Pile : public QObject {
public:
    virtual void setVisible(void);
};
class Spider {
    void dealRow();
    Pile *redeals[5];
    int m_redeal;
};
void Spider::dealRow()
{
    redeals[m_redeal++]->setVisible();
}
