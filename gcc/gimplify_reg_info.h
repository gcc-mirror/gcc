/* gimplify_reg_info is used during gimplification while walking over
   operands and their corresponding constraints of asm statements in order to
   detect errors.

   m_alt_output is a mapping describing which registers are potentially used in
   which alternative over all outputs.  Similarly for m_alt_input but over all
   inputs.

   m_early_clobbered_alt is a mapping describing which register is early
   clobbered in which alternative over all outputs.

   m_early_clobbered_output is the counter part to the prior one, i.e., it
   is a mapping describing which register is early clobbered in which operand
   over all alternatives.

   m_reg_asm_output is the set of registers (including register pairs) used for
   register asm output operands.

   m_reg_asm_input similar as m_reg_asm_output but for inputs.  */

#include "regs.h"

class gimplify_reg_info
{
  HARD_REG_SET *m_buf;
  HARD_REG_SET *m_alt_output;
  HARD_REG_SET *m_alt_input;
  HARD_REG_SET *m_early_clobbered_alt;
  HARD_REG_SET *m_early_clobbered_output;
  HARD_REG_SET m_reg_asm_output;
  HARD_REG_SET m_reg_asm_input;
  const unsigned m_num_alternatives;
  const unsigned m_num_outputs;
  /* Member m_clobbered describes all the registers marked as clobbered in an
     asm statement, i.e., this is the clobbers list of an extended asm

     asm asm-qualifiers ( AssemblerTemplate
		 : OutputOperands
		 [ : InputOperands
		 [ : Clobbers ] ])

     and is not to be confused with the early clobbers sets.  */
  HARD_REG_SET m_clobbered;

  /* Return the first overlapping register of REGS and REGNO:MODE or -1.  */
  int test (const HARD_REG_SET &regs, int regno) const
  {
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));

    if (TEST_HARD_REG_BIT (regs, regno))
      return regno;

    int end_regno = end_hard_regno (mode, regno);
    while (++regno < end_regno)
      if (TEST_HARD_REG_BIT (regs, regno))
	return regno;

    return -1;
  }

public:
  tree operand;

  gimplify_reg_info (unsigned num_alternatives,
		     unsigned num_outputs)
    : m_num_alternatives{num_alternatives}
    , m_num_outputs{num_outputs}
  {
    CLEAR_HARD_REG_SET (m_reg_asm_output);
    CLEAR_HARD_REG_SET (m_reg_asm_input);
    CLEAR_HARD_REG_SET (m_clobbered);

    /* If there are no alternatives, then there are no outputs/inputs and there
       is nothing to do on our end.  Thus, we are dealing most likely with a
       basic asm.  */
    if (num_alternatives == 0)
      return;

    unsigned buf_size = num_alternatives * 3 + num_outputs;
    m_buf = new HARD_REG_SET[buf_size];
    for (unsigned i = 0; i < buf_size; ++i)
      CLEAR_HARD_REG_SET (m_buf[i]);
    m_alt_output = &m_buf[0];
    m_alt_input = &m_buf[num_alternatives];
    m_early_clobbered_alt = &m_buf[num_alternatives * 2];
    if (num_outputs > 0)
      m_early_clobbered_output = &m_buf[num_alternatives * 3];
    else
      m_early_clobbered_output = nullptr;
  }

  ~gimplify_reg_info ()
  {
    if (m_num_alternatives > 0)
      delete[] m_buf;
  }

  void set_output (unsigned alt, int regno)
  {
    gcc_checking_assert (alt < m_num_alternatives);
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));
    add_to_hard_reg_set (&m_alt_output[alt], mode, regno);
  }

  void set_input (unsigned alt, int regno)
  {
    gcc_checking_assert (alt < m_num_alternatives);
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));
    add_to_hard_reg_set (&m_alt_input[alt], mode, regno);
  }

  int test_alt_output (unsigned alt, int regno) const
  {
    gcc_checking_assert (alt < m_num_alternatives);
    return test (m_alt_output[alt], regno);
  }

  int test_alt_input (unsigned alt, int regno) const
  {
    gcc_checking_assert (alt < m_num_alternatives);
    return test (m_alt_input[alt], regno);
  }

  void set_reg_asm_output (int regno)
  {
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));
    add_to_hard_reg_set (&m_reg_asm_output, mode, regno);
  }

  int test_reg_asm_output (int regno) const
  {
    return test (m_reg_asm_output, regno);
  }

  void set_reg_asm_input (int regno)
  {
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));
    add_to_hard_reg_set (&m_reg_asm_input, mode, regno);
  }

  int test_reg_asm_input (int regno) const
  {
    return test (m_reg_asm_input, regno);
  }

  void set_early_clobbered (unsigned alt, unsigned output, int regno)
  {
    gcc_checking_assert (alt < m_num_alternatives);
    gcc_checking_assert (output < m_num_outputs);
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));
    add_to_hard_reg_set (&m_early_clobbered_alt[alt], mode, regno);
    add_to_hard_reg_set (&m_early_clobbered_output[output], mode, regno);
  }

  bool test_early_clobbered_alt (unsigned alt, int regno) const
  {
    gcc_checking_assert (alt < m_num_alternatives);
    return TEST_HARD_REG_BIT (m_early_clobbered_alt[alt], regno);
  }

  bool is_early_clobbered_in_any_output_unequal (unsigned operand,
						 int regno) const
  {
    gcc_checking_assert (operand < m_num_outputs);
    for (unsigned op = 0; op < m_num_outputs; ++op)
      if (op != operand
	  && TEST_HARD_REG_BIT (m_early_clobbered_output[op], regno))
	return true;
    return false;
  }

  void set_clobbered (int regno)
  {
    SET_HARD_REG_BIT (m_clobbered, regno);
  }

  bool is_clobbered (int regno) const
  {
    machine_mode mode = TYPE_MODE (TREE_TYPE (operand));
    return overlaps_hard_reg_set_p (m_clobbered, mode, regno);
  }
};
