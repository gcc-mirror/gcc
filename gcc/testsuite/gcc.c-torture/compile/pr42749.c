struct pdf_object { int val; };
int pdf_count_size_object (struct pdf_object * p_obj)
{
    return pdf_count_size_object(p_obj) + 2 * sizeof(struct pdf_object);
}
