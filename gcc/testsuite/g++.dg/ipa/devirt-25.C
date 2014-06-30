/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp"  } */
/* { dg-add-options bind_pic_locally } */

class ert_RefCounter {
 protected:
  int refCounterE;
  virtual ~ert_RefCounter() {}
};

class ebs_Object : virtual public ert_RefCounter {
};

class dpr_App : public ebs_Object {
 public:
  virtual void run();
};

class dpr_Job : public ebs_Object {};

void dpr_run(ebs_Object& objectA) {
  ((dpr_App&)objectA).run();
  dpr_Job jobL;
  dpr_run(jobL);
}
/* { dg-final { scan-ipa-dump "Type inconsistent devirtualization" "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
