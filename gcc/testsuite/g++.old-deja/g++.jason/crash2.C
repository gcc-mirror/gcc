// Bug: g++ crashes on this (admittedly invalid) input.
// Special g++ Options:
// Build don't link:

class PhysicalPageId {
  const maximum_block_numbers = 2;
  long block_number[maximum_block_numbers];
};

const PhysicalPageId shadows_physical_page_id_null = { 2, { 0, 0 } }; // ERROR - constructor initializes non-field m_b_n
